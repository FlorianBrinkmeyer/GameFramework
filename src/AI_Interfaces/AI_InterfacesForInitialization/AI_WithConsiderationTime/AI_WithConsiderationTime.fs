namespace GameFramework

type AI_WithConsiderationTime =
    abstract Player : int
    abstract ConsiderationTime : int with get, set