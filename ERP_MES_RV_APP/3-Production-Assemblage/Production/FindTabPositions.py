def FindTabPositions(height, width, backRadius, frontRadius):
    leftTab = (0, (height - backRadius) / 2)
    if height - backRadius < 50:
        leftTab = None
    rightTab = (width, (height - frontRadius) / 2)
    if height - frontRadius < 50:
        rightTab = None
    topFlatLenght = width - backRadius - frontRadius
    topTab = (backRadius + topFlatLenght/2, height)
    if topFlatLenght < 50:
        topTab = None
    bottomTab = (width/2, 0)
    tabs = [tab for tab in [leftTab, topTab, rightTab, bottomTab] if tab is not None]
    return tabs

def FindBeamTabPositions(height, width):
    leftTab = (0, height/2)
    rightTab = (width, height/2)
    topTab = (width/2, height)
    bottomTab = (width/2, 0)
    return [leftTab, topTab, rightTab, bottomTab]