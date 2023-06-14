import os
from ezdxf import recover



import ezdxf

def format_dxf_for_laser_cutting(input_dxf_path, output_dxf_path, text, tab_coordinates,tabsize=25):
    # Load the existing DXF document
    project_root = os.path.dirname(os.path.abspath(__file__))
    path = os.path.join(project_root, input_dxf_path)
    doc, _ = recover.readfile(path)

    doc.dxfversion = "R2018"
    doc.saveas("R2018.dxf")
    doc.saveas(output_dxf_path)

    doc = ezdxf.readfile(output_dxf_path)
    # Get the modelspace of the document
    msp = doc.modelspace()
    min_point = (float('inf'), float('inf'))
    max_point = (float('-inf'), float('-inf'))

    #Identify dxf dimensions (for testing purposes only)
    for entity in msp:
        if entity.dxftype() == 'LINE':
            start = entity.dxf.start
            end = entity.dxf.end

            # Update the bounding box based on the entity coordinates
            if min_point is None or start[0] < min_point[0] or start[1] < min_point[1]:
                min_point = (min(start[0], min_point[0]), min(start[1], min_point[1]))
            if max_point is None or end[0] > max_point[0] or end[1] > max_point[1]:
                max_point = (max(end[0], max_point[0]), max(end[1], max_point[1]))
    print(min_point)
    print(max_point)

    # Define the rectangle coordinates and attributes
    width = tabsize
    height = tabsize
    #x = 50
    #y = -10

    # Define the hiding rectangle attributes
    layer = "0"  # Create a new layer for the hiding rectangle
    color = 7  # Color: 7 for white

    # Create the hiding rectangle as a wipeout on the hiding layer
    #wipeout = msp.add_wipeout([(x, y), (x + width, y + height)])
    offset_x = 50
    offset_y = 50
    for coordinates in tab_coordinates:
        x = coordinates[0]+50
        y = coordinates[1]+50
        e = msp.add_wipeout([(x - width/2, y - height/2), (x + width/2, y + height/2)])

    # Add the text to the dxf
    text = msp.add_text(text, dxfattribs={
        'insert': (-40, -40),  # Text insertion point
        'height': 8,  # Text height
        'style': 'Arial Narrow'
    })

    # Offset the dxf
    offset_dxf(doc,offset_x,offset_y)
    # Save the modified DXF file
    doc.saveas(output_dxf_path)



def offset_dxf(dxf, offset_x, offset_y):
    for entity in dxf.entities:
        if entity.dxftype() == 'TEXT':
            entity.dxf.insert = (entity.dxf.insert[0] + offset_x, entity.dxf.insert[1] + offset_y)
        elif entity.dxftype() == 'LINE':
            entity.dxf.start = (entity.dxf.start[0] + offset_x, entity.dxf.start[1] + offset_y)
            entity.dxf.end = (entity.dxf.end[0] + offset_x, entity.dxf.end[1] + offset_y)


# Usage example
# input_dxf_path = 'Test/top_view.dxf'
# output_dxf_path = 'Test/output_with_tab_curvetest.dxf'
# format_dxf_for_laser_cutting(input_dxf_path, output_dxf_path,'123456/1',[(50, -10)])
