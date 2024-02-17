# Function to create the fullscreen button UI along with its JavaScript functionality
createFullscreenToggleFeature <- function(element_id) {
  ns = NS(element_id)
  btn_id <- ns("fullscreenBtn")
  
  # Define the UI button for fullscreen toggle
  btn_ui <- actionButton(
    btn_id, 
    label = "", 
    icon = icon("expand"),
    style = "position: absolute; top: 10px; right: 10px; background-color: transparent; border: none;"
  )
  
  # Define the JavaScript script for the fullscreen toggle feature
  js_code <- sprintf(
    "
    $(document).on('click', '#%s', function() {
      var elem = $('#%s');
      elem.css('background-color', 'white');
      
      // Cross-browser support for requesting fullscreen mode
      function requestFullScreen(element) {
        if (element.requestFullscreen) {
          element.requestFullscreen();
        } else if (element.mozRequestFullScreen) { /* Firefox */
          element.mozRequestFullScreen();
        } else if (element.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
          element.webkitRequestFullscreen();
        } else if (element.msRequestFullscreen) { /* IE/Edge */
          element.msRequestFullscreen();
        }
      }
      
      // Cross-browser support for exiting fullscreen mode
      function exitFullScreen() {
        if (document.exitFullscreen) {
          document.exitFullscreen();
        } else if (document.mozCancelFullScreen) { /* Firefox */
          document.mozCancelFullScreen();
        } else if (document.webkitExitFullscreen) { /* Chrome, Safari & Opera */
          document.webkitExitFullscreen();
        } else if (document.msExitFullscreen) { /* IE/Edge */
          document.msExitFullscreen();
        }
      }

      // Toggle fullscreen and adjust styles accordingly
      if (!document.fullscreenElement) {
        requestFullScreen(elem[0]);
        elem.css('width', '100%%');
        elem.css('overflow', 'auto');
        elem.children().first().css('width', '100%%');
        $('#%s').find('i').removeClass('fa-expand').addClass('fa-compress');
      } else {
        exitFullScreen();
        elem.css('width', '');
        elem.css('overflow', '');
        elem.children().first().css('width', '');
        $('#%s').find('i').removeClass('fa-compress').addClass('fa-expand');
      }
    });
    ", btn_id, element_id, btn_id, btn_id)
  
  btn_script <- tags$script(HTML(js_code))
  
  # Return a tag list with both the button UI and its associated script
  return(tagList(btn_ui, btn_script))
}

# Usage in UI:
# createFullscreenToggleFeature("targetElementHTMLRenderedId")
