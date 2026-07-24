__all__ = ["CourseSiteConfig", "main"]


def __getattr__(name):
    if name in __all__:
        from .schedule import CourseSiteConfig, main

        exports = {"CourseSiteConfig": CourseSiteConfig, "main": main}
        return exports[name]
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
