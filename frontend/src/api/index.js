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
      .then(async res => {
        const data = await res.json();

        if(!res.ok) {
          const err = new Error(`Fetch error: ${res.statusText}`);
          err.response = res;
          err.body = data;
          throw err;
        }
        else {
          return data;
        }
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

export function getUserInfoForUser(userId) {
  return fetchJsonObs(apiUrl + '/user-info/' + userId);
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
  const channelName = `${userId.replace(/\|/, ';')}-user-info`;
  return rxjs.merge(
    getUserInfo(),
    channelEventObs(channelPool, channelName, 'update-user-info')
  );
}

export function getUserInfoForUserUpdates(channelPool, userId) {
  const channelName = `${userId.replace(/\|/, ';')}-user-info`;
  return rxjs.merge(
    getUserInfoForUser(userId),
    channelEventObs(channelPool, channelName, 'update-user-info')
  );
}

export function getInvites() {
  return fetchJsonObs(apiUrl + '/invites');
}

export function getInvitesUpdates(channelPool) {
  const userId = authService.userId;
  const channelName = `private-${userId.replace(/\|/, ';')}-invites`;
  return rxjs.merge(
    getInvites(),
    channelEventObs(channelPool, channelName, 'update-invites')
      .pipe(flatMap(() => getInvites()))
  );
}

export function sendInviteToUsername(otherUsername) {
  return fetchJsonObs(apiUrl + '/invites', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json; charset=utf-8'
    },
    body: JSON.stringify({ username: otherUsername })
  });
}

export function acceptInvite(inviteId) {
  return fetchJsonObs(apiUrl + '/invites/accept', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json; charset=utf-8'
    },
    body: JSON.stringify({ id: inviteId })
  });
}

export function getGames() {
  return fetchJsonObs(apiUrl + '/games');
}

export function getGamesUpdates(channelPool) {
  const userId = authService.userId;
  const channelName = `private-${userId.replace(/\|/, ';')}-invites`;
  return rxjs.merge(
    getGames(),
    channelEventObs(channelPool, channelName, 'update-invites')
      .pipe(flatMap(() => getGames()))
  );
}

export function getGame(gameId) {
  return fetchJsonObs(apiUrl + '/games/' + gameId);
}

export function getGameUpdates(channelPool, gameId) {
  const channelName = `game-${gameId}`;
  return rxjs.merge(
    getGame(gameId),
    channelEventObs(channelPool, channelName, 'update-state')
      .pipe(flatMap(() => getGame(gameId)))
  );
}

export function performMove(gameId, body) {
  return fetchJsonObs(apiUrl + '/games/' + gameId, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json; charset=utf-8'
    },
    body: JSON.stringify(body)
  });
}
