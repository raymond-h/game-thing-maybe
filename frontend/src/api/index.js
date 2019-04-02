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

    return fetch(url, outOpts).then(res => res.json());
  });
}

const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

export function someJson() {
  return fetchJsonObs(apiUrl + '/some-json');
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

export function getUserInfoUpdates(userInfoChan) {
  return rxjs.merge(
    getUserInfo(),
    rxjs.fromEventPattern(
      handler => userInfoChan.bind('update-user-info', handler),
      handler => userInfoChan.unbind('update-user-info', handler)
    )
      .pipe(
        flatMap(() => getUserInfo())
      )
  );
}
