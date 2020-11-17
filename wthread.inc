// add total debug information using the wlog module
// to build the demo you need to enable (remove the dot)
{.$DEFINE WTHREAD_DEBUG_LOG}

// "forced" switching of the module to the "library" mode,
// without using windows (Windows OS) functions
{$UNDEF WTHREAD_WINDOWS}
{$DEFINE WTHREAD_LIBRARY}

