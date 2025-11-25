/**
 * Plotly Event Handlers for Trainset Application
 * Handles selection and click events with Ctrl/Cmd key detection for label clearing
 */

function setupPlotlyEventHandlers(el) {
  // Send ctrl state when selection occurs - capture at moment of event
  el.on("plotly_selected", function (eventData) {
    var ctrlPressed = false;
    // Check for Ctrl/Cmd at the exact moment of the event
    if (typeof event !== "undefined" && event) {
      ctrlPressed = event.ctrlKey || event.metaKey;
    } else {
      // Fallback: check current key states
      ctrlPressed = window.event ? window.event.ctrlKey || window.event.metaKey : false;
    }

    // Extract only needed data to avoid circular references
    var cleanEventData = null;
    if (eventData && eventData.points && Array.isArray(eventData.points)) {
      cleanEventData = eventData.points.map(function (point) {
        return {
          pointNumber: point.pointNumber != null ? point.pointNumber : 0,
          curveNumber: point.curveNumber != null ? point.curveNumber : 0,
          x: point.x != null ? point.x : null,
          y: point.y != null ? point.y : null,
        };
      });
    }

    // Send combined event data with key state
    Shiny.setInputValue(
      "plotly_selected_with_keys",
      {
        points: cleanEventData,
        ctrlPressed: ctrlPressed,
        timestamp: Date.now(),
      },
      { priority: "event" }
    );
  });

  // Also handle click events
  el.on("plotly_click", function (eventData) {
    var ctrlPressed = false;
    // Check for Ctrl/Cmd at the exact moment of the event
    if (typeof event !== "undefined" && event) {
      ctrlPressed = event.ctrlKey || event.metaKey;
    } else {
      // Fallback: check current key states
      ctrlPressed = window.event ? window.event.ctrlKey || window.event.metaKey : false;
    }

    // Extract only needed data to avoid circular references
    var cleanEventData = null;
    if (
      eventData &&
      eventData.points &&
      Array.isArray(eventData.points) &&
      eventData.points.length > 0
    ) {
      var point = eventData.points[0];
      cleanEventData = [
        {
          pointNumber: point.pointNumber != null ? point.pointNumber : 0,
          curveNumber: point.curveNumber != null ? point.curveNumber : 0,
          x: point.x != null ? point.x : null,
          y: point.y != null ? point.y : null,
        },
      ];
    }

    // Send combined event data with key state
    Shiny.setInputValue(
      "plotly_click_with_keys",
      {
        points: cleanEventData,
        ctrlPressed: ctrlPressed,
        timestamp: Date.now(),
      },
      { priority: "event" }
    );
  });
}
