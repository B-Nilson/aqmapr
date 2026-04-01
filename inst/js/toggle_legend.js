
function toggleLegend(groupName, show) {
    let id = groupName.replaceAll(' ', '_').replaceAll('.', '_').replaceAll(',', '_').replaceAll("'", '_');
    let legend = document.querySelector('#' + id);
    legend.style.display = show ? 'block' : 'none';
}
