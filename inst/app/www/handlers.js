/**
 * Custom handlers para PAS.150
 */

$(document).on('shiny:connected', function(event) {
  console.log('PAS.150: Conexión con el servidor establecida.');
});

Shiny.addCustomMessageHandler('log_console', function(message) {
  console.log('Mensaje desde R:', message);
});

let currentDragTarget = null;
let startX, startY, scrollLeft, scrollTop;

// Cambiar cursor al presionar/soltar Control
$(document).on('keydown', function(e) {
    if (e.key === 'Control') $('.container_zoom').css('cursor', 'grab');
});
$(document).on('keyup', function(e) {
    if (e.key === 'Control') $('.container_zoom').css('cursor', 'default');
});

// Manejo de Zoom
$(document).on('wheel', '.container_zoom', function(e) {
    if (e.ctrlKey) {
    e.preventDefault();
    let table = $(this).find('.inner_table');
    
    // Obtener escala actual o definir inicial
    let scale = table.data('scale') || 1;
    
    // Guardar dimensiones naturales (sin escala) para cálculos de scroll precisos
    if (!table.data('nw')) {
        table.data('nw', table[0].offsetWidth / scale);
        table.data('nh', table[0].offsetHeight / scale);
    }
    let nw = table.data('nw');
    let nh = table.data('nh');

    // Calcular nueva escala (sensibilidad de 0.1)
    if (e.originalEvent.deltaY < 0) {
        scale += 0.1;
    } else {
        scale = Math.max(1, scale - 0.1); // No permite zoom menor al tamaño original (1)
    }
    
    // Aplicar escala y forzar alineación y márgenes.
    // Forzar 'margin-left: 0' evita que contenedores que centran el contenido (flexbox)
    // recorten el lado izquierdo de la tabla al desbordar.
    table.css({
      'transform': 'scale(' + scale + ')',
      'transform-origin': '0 0',
      'margin-left': scale > 1 ? '0' : '',
      'margin-top': scale > 1 ? '0' : '',
      // "Empujamos" los límites del contenedor usando márgenes para habilitar el scroll
      'margin-right': scale > 1 ? (nw * (scale - 1)) + 'px' : '',
      'margin-bottom': scale > 1 ? (nh * (scale - 1)) + 'px' : '',
      'min-width': nw + 'px', 
      'min-height': nh + 'px'
    });

    table.data('scale', scale);
    }
});

// Inicio del arrastre (Pan)
$(document).on('mousedown', '.container_zoom', function(e) {
    if (e.ctrlKey) {
    currentDragTarget = this;
    $(this).css('cursor', 'grabbing');
    startX = e.pageX - this.offsetLeft;
    startY = e.pageY - this.offsetTop;
    scrollLeft = this.scrollLeft;
    scrollTop = this.scrollTop;
    return false; // Evitar selección de texto
    }
});

// Movimiento del arrastre
$(document).on('mousemove', function(e) {
    if (!currentDragTarget) return;
    let container = currentDragTarget;
    const x = e.pageX - container.offsetLeft;
    const y = e.pageY - container.offsetTop;
    container.scrollLeft = scrollLeft - (x - startX);
    container.scrollTop = scrollTop - (y - startY);
});

// Fin del arrastre
$(document).on('mouseup', function(e) {
    if (currentDragTarget) {
    $(currentDragTarget).css('cursor', e.ctrlKey ? 'grab' : 'default');
    currentDragTarget = null;
    }
});