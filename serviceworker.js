// キャッシュ名とキャッシュファイルの指定
const CACHE_NAME = 'mkpasswdpwa';
const urlsToCache = [
    '/mkpasswdpwa/',
    '/mkpasswdpwa/css/style.css',
    '/mkpasswdpwa/app.js'
];

self.addEventListener('install', event =>
    event.waitUntil(
        caches
            .open(CACHE_NAME)
            .then(cache => cache.addAll(urlsToCache))
    )
);

self.addEventListener('fetch', event =>
    event.respondWith(
        caches
            .match(event.request)
            .then(response => response ? response : fetch(event.request))
    )
);
