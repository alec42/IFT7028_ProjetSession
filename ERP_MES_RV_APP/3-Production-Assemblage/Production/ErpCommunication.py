import os
import io
import json
import shutil
from googleapiclient.discovery import build
from googleapiclient.http import MediaIoBaseDownload, MediaFileUpload
from google.oauth2 import service_account
import pandas as pd
import export_instructions
import export_dxfs

#region Public Methods for team 2-ERP

def GetProductionInformation( currentOrderId, firstPannelId, firstPieceId ):
    DownloadCurrentOrderJSON( currentOrderId )
    DownloadCurrentOrderFiles( currentOrderId )
    export_dxfs.export_all_dxfs(currentOrderId)
    uploadedFiles = UploadFiles( currentOrderId, '1rEnyXZtbNq3IC1EcS6eIh7DboE_L3hB3', './Production_Drawings' )
    pieceDataFrame, pannelIdProdFileAssociation = GetPieceDetailDataframe(currentOrderId, firstPannelId, firstPieceId)
    return pieceDataFrame, pannelIdProdFileAssociation

def GetAssemblyInformation( currentOrderId ):
    export_instructions.create_teardrop_assembly_manual(currentOrderId)
    UploadFiles( currentOrderId, '1b7Q763naO6kDTX9E03EJ19a6iz0_6XPX', './Instructions' )
    return f'Industrie_VR_IFT7028/Assembly/{currentOrderId}/Manuel_Assemblage.pdf'

#endregion

#============================================================================================================

#region Private Methods

def GetPieceDetailDataframe( currentOrderId, firstPannelId, firstPieceId ):
    cadFolder = './OpenSCAD_Parts'
    if not os.path.exists(cadFolder):
        print(f"Folder '{cadFolder}' does not exist.")
        return

    if not os.path.isdir(cadFolder):
        print(f"'{cadFolder}' is not a valid folder path.")
        return
    
    df = pd.DataFrame(columns=["CommandeID", "PanneauID", "PieceID", "PanneauType", "Fichier3D"])
    baseRemotePath = 'Industrie_VR_IFT7028/commandes_3d'
    files = os.listdir(cadFolder)
    scadFiles = [file for file in files if file.endswith('.scad')]
    trussCount = GetTrussCount(currentOrderId)
    pannelDxfAssociation = []
    pannelCount = 0
    pieceCount = 0
    for scadFile in scadFiles:
        if 'truss' in scadFile:
            trussPannelId = int(firstPannelId)+pannelCount
            for i in range(trussCount):
                row = {"CommandeID": currentOrderId, "PanneauID": trussPannelId, "PieceID": int(firstPieceId)+pieceCount, "PanneauType": 1, "Fichier3D": f"{baseRemotePath}/{scadFile}"}
                pannelDxfAssociation.append((trussPannelId, f"Industrie_VR_IFT7028/Production/{currentOrderId}/{scadFile.replace('.scad', '.dxf', 1)}"))
                pieceCount += 1
                df = pd.concat([df, pd.DataFrame([row])], ignore_index=True)

            pannelCount += 1
        else:
            row = {"CommandeID": currentOrderId, "PanneauID": int(firstPannelId)+pannelCount, "PieceID": int(firstPieceId)+pieceCount, "PanneauType": 1, "Fichier3D": f"{baseRemotePath}/{scadFile}"}
            pannelDxfAssociation.append((int(firstPannelId)+pannelCount, f"Industrie_VR_IFT7028/Production/{currentOrderId}/{scadFile.replace('.scad', '.dxf', 1)}"))
            pannelCount += 1
            pieceCount += 1
            df = pd.concat([df, pd.DataFrame([row])], ignore_index=True)

    return df, pannelDxfAssociation

def GetTrussCount( currentOrderId ):
    jsonFilePath = f'./OpenSCAD_Parts/{currentOrderId}_commande.json'
    with open(jsonFilePath) as file:
        json_data = json.load(file)
    model_params = json_data["FichiersFabrication"]
    num_fixed_trusses = sum(key.startswith("fixed_trusses") for key in model_params.keys())
    return num_fixed_trusses

def UploadFiles( currentOrderId, parentId, localFolder ):
    service = GetWriteAccessService()
    remoteFolderId = GetRemoteFolderId(parentId, currentOrderId)
    if not os.path.exists(localFolder):
        print(f"Folder '{localFolder}' does not exist.")
        return

    if not os.path.isdir(localFolder):
        print(f"'{localFolder}' is not a valid folder path.")
        return
    
    files = os.listdir(localFolder)
    for file in files:
        file_path = os.path.join(localFolder, file)
        if os.path.isfile(file_path):
            # Check if the file already exists in the remote folder
            existing_files = service.files().list(q=f"'{remoteFolderId}' in parents and name = '{file}'").execute()
            if existing_files.get('files'):
                # File exists, get its ID
                file_id = existing_files['files'][0]['id']

                # Update the file with the new version
                file_metadata = {'name': file}
                media = MediaFileUpload(file_path)
                updated_file = service.files().update(fileId=file_id, body=file_metadata, media_body=media).execute()
            else:
                # File does not exist, upload it as a new file
                file_metadata = {'name': file, 'parents': [remoteFolderId]}
                media = MediaFileUpload(file_path)
                uploaded_file = service.files().create(body=file_metadata, media_body=media).execute()
                
    return files

def GetRemoteFolderId( parentId, folderName ):
    service = GetReadOnlyService()
    results = service.files().list(q=f"name='{folderName}' and '{parentId}' in parents and trashed=false",
                                   spaces='drive',
                                   fields='files(id)').execute()
    files = results.get('files', [])
    folderExists = len(files) > 0
    if folderExists:
        return files[0]['id']
    
    folderMetadata = {
        'name': folderName,
        'mimeType': 'application/vnd.google-apps.folder',
        'parents': [parentId]
    }
    remoteFolder = service.files().create(body=folderMetadata, fields='id').execute()
    return remoteFolder['id']

def DownloadCurrentOrderJSON( currentOrderId ):
    service = GetReadOnlyService()
    rootId = '1BXCukUhixd3BrbS_Q6dWNYmUoNk8SHoL' # importee
    # rootId = '1PXBSvX-FZgWrKjxj5m9haGFTdSrB-V8u' # commandee
    query = f"name='{currentOrderId}.json' and '{rootId}' in parents and trashed=false"
    results = service.files().list(q=query, spaces='drive', fields='files(id,name)').execute()
    files = results.get('files', [])
    if len(files) == 0:
        raise ValueError(f"File '{currentOrderId}.json' not found")
    print(files)
    
    fileId = files[0]['id']
    fileName = files[0]['name']
    filePath = f'./OpenSCAD_Parts/{fileName}'
    request = service.files().get_media(fileId=fileId)
    fh = io.FileIO(filePath, 'wb')
    downloader = MediaIoBaseDownload(fh, request)
    done = False
    while not done:
        status, done = downloader.next_chunk()

def DownloadCurrentOrderFiles( currentOrderId ):
    service = GetReadOnlyService()
    rootId = '1twSfqdRPXtUeZ8tAhrHqHUkyq-XMaAp8'
    query = f"'{rootId}' in parents and trashed=false and mimeType='application/vnd.google-apps.folder' and name='{currentOrderId}'"
    folder = service.files().list(q=query).execute().get('files', [])
    if not folder:
        raise ValueError(f"Folder '{currentOrderId}' not found")

    folderId = folder[0]['id']
    outputFolderName = 'OpenSCAD_Parts'
    CreateOutputFolder(outputFolderName)
    files = service.files().list(q=f"'{folderId}' in parents and trashed=false").execute().get('files', [])
    for file in files:
        fileId = file['id']
        fileName = file['name']
        filePath = f'./{outputFolderName}/{fileName}'
        request = service.files().get_media(fileId=fileId)
        fh = io.FileIO(filePath, 'wb')
        downloader = MediaIoBaseDownload(fh, request)
        done = False
        while not done:
            status, done = downloader.next_chunk()
        
        print(f"Downloaded file: {fileName}")
    print("All files downloaded successfully.")
    
def CreateOutputFolder( folderName ):
    # Check if the folder doesn't exists
    if not os.path.exists(folderName):
        # Create the folder
        os.makedirs(folderName)
        print(f"Folder '{folderName}' created.")
        return
    
    print(f"Folder '{folderName}' already exists. Removing existing files and subfolders...")

    # Remove all files and subfolders within the existing folder
    for root, dirs, files in os.walk(folderName, topdown=False):
        for file in files:
            file_path = os.path.join(root, file)
            os.remove(file_path)
        for dir in dirs:
            dir_path = os.path.join(root, dir)
            shutil.rmtree(dir_path)

    print("Existing files and subfolders removed.")

def GetReadOnlyService():
    credentials = service_account.Credentials.from_service_account_file('./ift-7028-389418-5c740eb054da.json')
    scopes = ['https://www.googleapis.com/auth/drive.readonly', "https://www.googleapis.com/auth/drive.file"]
    credentials = credentials.with_scopes(scopes)
    service = build('drive', 'v3', credentials=credentials)
    return service

def GetWriteAccessService():
    credentialFile = './ift-7028-389418-5c740eb054da.json'
    scopes = ["https://www.googleapis.com/auth/drive.file"]
    credentials = service_account.Credentials.from_service_account_file(credentialFile, scopes=scopes)
    service = build('drive', 'v3', credentials=credentials)
    return service

#endregion

#DownloadCurrentOrderFiles(14870960)
#UploadProductionFiles(14870960)
# GetPieceDetailDataframe(14870960, 15, 2)
# a = GetProductionInformation(currentOrderId = '97297049', firstPannelId = 89, firstPieceId = 89)
# print(a)
# assembly = GetAssemblyInformation(currentOrderId = '97297049')
# assembly
