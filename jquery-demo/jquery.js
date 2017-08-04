window.jQuery = function (selectorOrElements) {
  var array = []

  if (typeof selectorOrElements === 'string') {
    var elements = document.querySelectorAll(selectorOrElements)

    for (var i = 0; i < elements.length; i++) {
      array.push(elements[i])
    }
  } else if(selectorOrElements instanceof Array) {
    for (var i = 0; i < selectorOrElements.length; i++) {
      array.push(selectorOrElements[i])
    }
    array['oldArray'] = selectorOrElements['oldArray']
  }
  


  array.on = function (eventType, fn) {
    for (var i = 0; i < array.length; i++) {
      array[i].addEventListener(eventType, fn)
    }
    return array    
  }
  array.addClass = function (className) {
    for (var i = 0; i < array.length; i++) {
      array[i].classList.add(className)
    }
    return array
  }
  array.removeClass = function (className) {
    for (var i = 0; i < array.length; i++) {
      array[i].classList.remove(className)
    }
    return array
  }
  array.toggleClass = function (className) {
    for (var i = 0; i < array.length; i++) {
      array[i].classList.toggle(className)
    }
    return array    
  }
  array.text = function (value) {
    if (value === undefined) {
      var result = []
      for (var i = 0; i < array.length; i++) {
        result.push(array[i].textContent)
      }
      return result
    } else {
      for (var i = 0; i < array.length; i++) {
        array[i].textContent = value
      }
      return array
    }  
  }
  array.html = function (htmlString) {
    if (htmlString === undefined) {
      var result = []
      for (var i = 0; i < array.length; i++) {
        result.push(array[i].innerHTML)
      }
      return result
    } else {
      for (var i = 0; i < array.length; i++) {
        array[i].innerHTML = htmlString
      }
      return array
    }
    
  }

  array.find = function (selector) {
    var array2 = []
    for (var i = 0; i < array.length; i++) {
      var elements = array[i].querySelectorAll(selector)
      for (var j = 0; j < elements.length; j++) {
        array2.push(elements[j])
      }
    }
    array2.oldArray = array
    return jQuery(array2)
  }

  array.end = function () {
    return array.oldArray
  }

  return array
}

window.$ = window.jQuery