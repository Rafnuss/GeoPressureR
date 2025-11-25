$(document).ready(function () {
  // Custom message handler to clear plotly selection
  Shiny.addCustomMessageHandler("clearPlotlySelection", function (plotId) {
    Plotly.restyle(plotId, { selectedpoints: [null] });
  });

  // Key detection for label clearing
  function setupKeyDetection(plotId) {
    var plotElement = document.getElementById(plotId);
    if (!plotElement) return;

    plotElement.on('plotly_selected', function(eventData) {
      var ctrlPressed = (event && (event.ctrlKey || event.metaKey)) || false;
      var cleanEventData = null;
      
      if (eventData && eventData.points && Array.isArray(eventData.points)) {
        cleanEventData = eventData.points.map(function(point) {
          return {
            pointNumber: point.pointNumber != null ? point.pointNumber : 0,
            curveNumber: point.curveNumber != null ? point.curveNumber : 0,
            x: point.x != null ? point.x : null,
            y: point.y != null ? point.y : null
          };
        });
      }
      
      Shiny.setInputValue('plotly_selected_with_keys', {
        points: cleanEventData,
        ctrlPressed: ctrlPressed,
        timestamp: Date.now()
      }, {priority: 'event'});
    });

    plotElement.on('plotly_click', function(eventData) {
      var ctrlPressed = (event && (event.ctrlKey || event.metaKey)) || false;
      var cleanEventData = null;
      
      if (eventData && eventData.points && Array.isArray(eventData.points) && eventData.points.length > 0) {
        var point = eventData.points[0];
        cleanEventData = [{
          pointNumber: point.pointNumber != null ? point.pointNumber : 0,
          curveNumber: point.curveNumber != null ? point.curveNumber : 0,
          x: point.x != null ? point.x : null,
          y: point.y != null ? point.y : null
        }];
      }
      
      Shiny.setInputValue('plotly_click_with_keys', {
        points: cleanEventData,
        ctrlPressed: ctrlPressed,
        timestamp: Date.now()
      }, {priority: 'event'});
    });
  }

  // Setup key detection when plot is ready
  $(document).on('shiny:value', function(event) {
    if (event.target.id === 'ts_plot') {
      setTimeout(function() {
        setupKeyDetection('ts_plot');
      }, 100);
    }
  });

  // Keyboard shortcuts
  $(document).keydown(function (e) {
    var plot = document.getElementById("ts_plot");
    if (plot && plot.data) {
      var currentRange = plot.layout.xaxis.range;
      if (currentRange) {
        var start = new Date(currentRange[0]);
        var end = new Date(currentRange[1]);
        var duration = end - start;
        var center = new Date(start.getTime() + duration / 2);

        // Zoom in with up arrow
        if (e.which === 38) {
          // Up arrow
          e.preventDefault();
          var newDuration = duration * 0.5; // Zoom in by 50%
          var newStart = new Date(center.getTime() - newDuration / 2);
          var newEnd = new Date(center.getTime() + newDuration / 2);
          Plotly.relayout("ts_plot", {
            "xaxis.range": [newStart, newEnd],
          });
        }
        // Zoom out with down arrow
        else if (e.which === 40) {
          // Down arrow
          e.preventDefault();
          var newDuration = duration * 2; // Zoom out by 100%
          var newStart = new Date(center.getTime() - newDuration / 2);
          var newEnd = new Date(center.getTime() + newDuration / 2);
          Plotly.relayout("ts_plot", {
            "xaxis.range": [newStart, newEnd],
          });
        }
        // Pan left with left arrow
        else if (e.which === 37) {
          // Left arrow
          e.preventDefault();
          var panAmount = e.shiftKey ? duration : duration * 0.1;
          var newStart = new Date(start.getTime() - panAmount);
          var newEnd = new Date(end.getTime() - panAmount);
          Plotly.relayout("ts_plot", {
            "xaxis.range": [newStart, newEnd],
          });
        }
        // Pan right with right arrow
        else if (e.which === 39) {
          // Right arrow
          e.preventDefault();
          var panAmount = e.shiftKey ? duration : duration * 0.1;
          var newStart = new Date(start.getTime() + panAmount);
          var newEnd = new Date(end.getTime() + panAmount);
          Plotly.relayout("ts_plot", {
            "xaxis.range": [newStart, newEnd],
          });
        }
      }
    }
  });

  // Custom mouse wheel handler for x-axis only zoom (unless over y-axis)
  $("#ts_plot").on("wheel", function (e) {
    e.preventDefault();
    var plot = document.getElementById("ts_plot");
    if (plot && plot.data) {
      var rect = plot.getBoundingClientRect();
      var x = e.clientX - rect.left;
      var y = e.clientY - rect.top;

      // Get plot area dimensions
      var plotWidth = rect.width;
      var plotHeight = rect.height;

      // Approximate y-axis areas (left and right sides)
      var leftYAxisWidth = 80; // Left y-axis area
      var rightYAxisWidth = 80; // Right y-axis area

      var zoomFactor = e.shiftKey ? 0.2 : 0.1; // Faster zoom with shift

      // Check if mouse is over left y-axis (pressure)
      if (x < leftYAxisWidth) {
        var currentYRange = plot.layout.yaxis.range;
        if (currentYRange) {
          var yStart = currentYRange[0];
          var yEnd = currentYRange[1];
          var yDuration = yEnd - yStart;
          var yCenter = yStart + yDuration / 2;

          var newYDuration;
          if (e.originalEvent.deltaY < 0) {
            newYDuration = yDuration * (1 - zoomFactor);
          } else {
            newYDuration = yDuration * (1 + zoomFactor);
          }

          var newYStart = yCenter - newYDuration / 2;
          var newYEnd = yCenter + newYDuration / 2;
          Plotly.relayout("ts_plot", {
            "yaxis.range": [newYStart, newYEnd],
          });
        }
      }
      // Check if mouse is over right y-axis (acceleration)
      else if (x > plotWidth - rightYAxisWidth) {
        var currentY2Range = plot.layout.yaxis2.range;
        if (currentY2Range) {
          var y2Start = currentY2Range[0];
          var y2End = currentY2Range[1];
          var y2Duration = y2End - y2Start;
          var y2Center = y2Start + y2Duration / 2;

          var newY2Duration;
          if (e.originalEvent.deltaY < 0) {
            newY2Duration = y2Duration * (1 - zoomFactor);
          } else {
            newY2Duration = y2Duration * (1 + zoomFactor);
          }

          var newY2Start = y2Center - newY2Duration / 2;
          var newY2End = y2Center + newY2Duration / 2;
          Plotly.relayout("ts_plot", {
            "yaxis2.range": [newY2Start, newY2End],
          });
        }
      }
      // Default: zoom x-axis when over plot area
      else {
        var currentRange = plot.layout.xaxis.range;
        if (currentRange) {
          var start = new Date(currentRange[0]);
          var end = new Date(currentRange[1]);
          var duration = end - start;
          var center = new Date(start.getTime() + duration / 2);

          var newDuration;
          if (e.originalEvent.deltaY < 0) {
            newDuration = duration * (1 - zoomFactor);
          } else {
            newDuration = duration * (1 + zoomFactor);
          }

          var newStart = new Date(center.getTime() - newDuration / 2);
          var newEnd = new Date(center.getTime() + newDuration / 2);
          Plotly.relayout("ts_plot", {
            "xaxis.range": [newStart, newEnd],
          });
        }
      }
    }
  });
});
