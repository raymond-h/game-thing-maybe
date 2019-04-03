import * as rxjs from 'rxjs';
import { flatMap } from 'rxjs/operators';

import authService from './auth';

function fetchJsonObs(url, opts = {}) {
  return rxjs.defer(() => {
    const outOpts = { ...opts };

    if(authService.isAuthenticated$.value) {
      outOpts.headers = outOpts.headers || {};
      outOpts.headers['Authorization'] = `Bearer ${authService.accessToken}`;
    }

    return fetch(url, outOpts)
      .then(res => {
        if(!res.ok) {
          throw new Error(`Fetch error: ${res.statusText}`);
        }

        return res.json();
      });
  });
}

function channelEventObs(channelPool, channelName, event) {
  return new rxjs.Observable(observer => {
    const handler = data => observer.next(data);

    const chan = channelPool.get(channelName);
    chan.bind(event, handler);
    chan.bind('pusher:subscription_error', status => {
      observer.error(new Error(`Error during subscription to channel '${channelName}': HTTP status code ${status}`));
    });

    return () => {
      chan.unbind(event, handler);
      channelPool.release(channelName);
    };
  });
}

const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

export function someJson() {
  return fetchJsonObs(apiUrl + '/some-json');
}

export function authenticatePusher(channel, socketId) {
  return fetchJsonObs(apiUrl + '/pusher/auth', {
    method: 'POST',
    body: new URLSearchParams({
      socket_id: socketId,
      channel_name: channel.name
    })
  });
}

export function getUserInfo() {
  return fetchJsonObs(apiUrl + '/user-info');
}

export function updateUserInfo(newUserInfo) {
  return fetchJsonObs(apiUrl + '/user-info', {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json; charset=utf-8'
    },
    body: JSON.stringify(newUserInfo)
  });
}

export function getUserInfoUpdates(channelPool, userId) {
  const channelName = `private-${userId.replace(/\|/, ';')}-user-info`;
  return rxjs.merge(
    getUserInfo(),
    channelEventObs(channelPool, channelName, 'update-user-info')
  );
}
