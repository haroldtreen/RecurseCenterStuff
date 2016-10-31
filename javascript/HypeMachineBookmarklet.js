(function() {
    function getTracks() {
        var tracks = [];
        $('.section-player').each(function() {
            tracks.push({
                title: $(this).find('.track_name').text().replace(/\s+/g, ' ').trim() + '.mp3',
                url: $(this).find('.dl').find('a').attr('href'),
            });
        });
        return tracks;
    }

    function downloadTrack(track) {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', track.url, true);
        xhr.responseType = 'blob';
        xhr.onload = function(e) {
            var link = document.createElement('a');
            link.download = track.title;
            link.href = (URL || webkitURL).createObjectURL(this.response);
            link.target = '_blank';

            document.body.appendChild(link);
            link.click();
        };
        xhr.send();
    }

    var downloadLimit = Number(prompt('How many tracks to download?'));
    getTracks().forEach(function(track, index) {
        if (index < downloadLimit) {
            downloadTrack(track);
        }
    });
}())
