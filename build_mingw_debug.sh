SOURCE_DIR =
if [ -n "$1" ]; then
  SOURCE_DIR="$1"

  cmake -Wno-dev      \
    -DCMAKE_BUILD_TYPE:STRING="Debug"  \
    -G 'Unix Makefiles'    \
    ${SOURCE_DIR}
else
  echo "Please input the source directory"
fi
