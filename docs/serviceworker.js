// キャッシュ名とキャッシュファイルの指定
var CACHE_NAME = 'mkpasswdpwa';
var urlsToCache = [
	'/mkpasswdpwa/',
	'/mkpasswdpwa/css/style.css',
	'/mkpasswdpwa/app.js'
];

self.addEventListener('install', function(event) {
	event.waitUntil(
		caches
			.open(CACHE_NAME)
			.then(function(cache) {
				return cache.addAll(urlsToCache);
			})
	);
});

self.addEventListener('fetch', function(event) {
	event.respondWith(
		caches
			.match(event.request)
			.then(function(response) {
				return response ? response : fetch(event.request);
			})
	);
});
