
/**
 * Draws a square on the given SVG element.
 * @param {SVGElement} svg - The SVG container to draw on.
 * @param {number} x - X-coordinate of the top-left corner.
 * @param {number} y - Y-coordinate of the top-left corner.
 * @param {number} size - Width and height of the square.
 * @param {string} color - Fill color.
 */
function drawSquare(svg, x, y, size, color = 'blue') {
    const square = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    square.setAttribute("x", x);
    square.setAttribute("y", y);
    square.setAttribute("width", size);
    square.setAttribute("height", size);
    square.setAttribute("fill", color);
    svg.appendChild(square);
  }
  
  /**
   * Draws an equilateral triangle on the given SVG element.
   * @param {SVGElement} svg - The SVG container to draw on.
   * @param {number} x - X-coordinate of the center of the triangle.
   * @param {number} y - Y-coordinate of the top vertex of the triangle.
   * @param {number} size - Length of one side.
   * @param {string} color - Fill color.
   */
  function drawTriangle(svg, x, y, size, color = 'red') {
    const height = (Math.sqrt(3) / 2) * size;
  
    const point1 = `${x},${y}`;
    const point2 = `${x - size / 2},${y + height}`;
    const point3 = `${x + size / 2},${y + height}`;
  
    const triangle = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
    triangle.setAttribute("points", `${point1} ${point2} ${point3}`);
    triangle.setAttribute("fill", color);
    svg.appendChild(triangle);
  }