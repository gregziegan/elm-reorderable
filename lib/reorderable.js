// most of this taken from https://github.com/NoRedInk/drag-and-drop
// The number of pixels in either the x or y directions you must drag something
// before it counts as being dragged (as opposed to a click)
var distanceToBeginDragging = 2;

// This is necessary to standardize what "clientX" and "clientY" refer to.
// The version we get from "touches" doesn't account for window scroll, whereas
// it does on mouse events. This way, both of them account for it.
function dragCoordinatesFromEvent (event) {
  var touches = event.touches;

  if (typeof touches === "object" && touches.length > 0) {
    var touch = touches[0];

    return {
      x: touch.clientX,
      y: touch.clientY
    };
  } else {
    return {
      x: event.clientX,
      y: event.clientY
    };
  }
}

function cursorCoordinatesFromEvent (event) {
  var touches = event.touches;

  if (typeof touches === "object" && touches.length > 0) {
    var touch = touches[0];

    return {
      x: touch.pageX - window.pageXOffset,
      y: touch.pageY - window.pageYOffset
    };
  } else {
    return {
      x: event.clientX,
      y: event.clientY
    };
  }
}

var dragAxis;

function setUpReorderableEvents(options) {
  var reorderableDataAttr = options.reorderableDataAttr;
  var reorderableQuerySelector = options.reorderableQuerySelector;
  var placeholderId = options.placeholderId;
  var onDragStart = options.on.dragStart;
  var onDragStop = options.on.dragStop;
  var onDragMove = options.on.dragMove;
  var offsets = options.offsets;
  var potentiallyDraggingFrom = null;
  var lastClientX = null;
  var lastClientY = null;
  var dragOffsetX = 0;
  var dragOffsetY = 0;
  var bounds = { top: 0, left: 0, width: 0, height: 0 };
  var lastMovementData = null;

  function getSourceIndex(elem) {
    var result = elem.dataset[reorderableDataAttr];

    if (result != null) {
      return parseInt(result, 10);
    } else {
      if (elem.parentElement) {
        return getSourceIndex(elem.parentElement);
      } else {
        return null;
      }
    }
  }

  function getDropZoneId(elem) {
    return parseInt(elem.dataset[reorderableDataAttr], 10);
  }

  function getReorderable(elem) {
    var curEl = elem;
    while (curEl &&
           typeof curEl.dataset !== "undefined" &&
           !curEl.dataset.hasOwnProperty(reorderableDataAttr)) {
      curEl = curEl.parentElement;
    }
    return curEl;
  }

  function attemptDragStart(offsetX, offsetY) {
    return function(event) {
      var reorderable = getReorderable(event.target)

      if(reorderable) {
        // Prevent the usual drag-to-select-text behavior on mousedown
        // as well as drag-to-pan on touchstart.
        event.preventDefault();

        var coordinates = dragCoordinatesFromEvent(event);
        var x = coordinates.x;
        var y = coordinates.y;

        bounds = reorderable.getBoundingClientRect();

        lastClientX = x;
        lastClientY = y;
        dragOffsetX = x - bounds.left + offsetX;
        dragOffsetY = y - bounds.top + offsetY;

        potentiallyDraggingFrom = reorderable;
      }
    }
  }

  document.addEventListener("mousedown",
    attemptDragStart(offsets.mouse.x, offsets.mouse.y));

  document.addEventListener("touchstart",
    attemptDragStart(offsets.touch.x, offsets.touch.y));

  function handleDragRelease(event) {
    if (potentiallyDraggingFrom === null && lastMovementData !== null) {
      var placeholder = lastMovementData.placeholder;
      var reorderableBounds = lastMovementData.reorderableBounds;
      onDragStop({
        placeholder: placeholder,
        reorderableBounds: reorderableBounds
      });
      lastMovementData = null;
    } else {
      potentiallyDraggingFrom = null;
    }
  }

  ["mouseup", "touchend", "touchcancel"].forEach(function(eventName) {
    document.addEventListener(eventName, handleDragRelease);
  });


  function handleDragMove(event) {
    var coordinates = dragCoordinatesFromEvent(event);
    var x = coordinates.x;
    var y = coordinates.y;

    if ((Math.abs(x - lastClientX) >= distanceToBeginDragging)
      || (Math.abs(y - lastClientY) >= distanceToBeginDragging)
      || (event instanceof MouseEvent && lastClientX === null && lastClientY === null)  // otherwise, initial hovers are skipped
    ) {
      var placeholderPoint;
      if (dragAxis === 'x') {
        placeholderPoint = {
          x: x - dragOffsetX,
          y: bounds.top
        };
      } else if (dragAxis === 'y') {
        placeholderPoint = {
          x: bounds.left,
          y: y - dragOffsetY
        };
      } else {
        placeholderPoint = {
          x: x - dragOffsetX,
          y: y - dragOffsetY
        }
      }

      var documentElement = event.target.ownerDocument.documentElement;
      var clientSize = {
        height: window.innerHeight,
        width: window.innerWidth
      };

      if (potentiallyDraggingFrom !== null) {
        potentiallyDraggingFrom = null;

        var sourceIndex = getSourceIndex(event.target);

        if (typeof sourceIndex === "number") {
          onDragStart({
            sourceIndex: sourceIndex,
            point: placeholderPoint,
          });
        }
      }

      var placeholder = document.getElementById(placeholderId);

      if (placeholder !== null) {
        var reorderableElems = document.querySelectorAll(reorderableQuerySelector);
        var reorderableBounds = []; // List BoundingClientRect

        for (var index = 0, length = reorderableElems.length; index < length; index++) {
          var reorderableElem = reorderableElems[index];

          reorderableBounds.push(
            reorderableElem.getBoundingClientRect()
          );
        }

        bounds = placeholder.getBoundingClientRect();

        var moveData = {
          clientSize: clientSize,
          cursor: cursorCoordinatesFromEvent(event),
          placeholder: {
            point: placeholderPoint,
            bounds: bounds
          },
          reorderableBounds: reorderableBounds,
        };

        lastMovementData = moveData;

        onDragMove(moveData);
      }
    }
  }

  ["mousemove", "touchmove"].forEach(function(eventName) {
    document.addEventListener(eventName, handleDragMove);
  });
};

function setDragAxis(newAxis) {
  dragAxis = newAxis;
}

Reorderable = { setUpReorderableEvents: setUpReorderableEvents, setDragAxis: setDragAxis };
