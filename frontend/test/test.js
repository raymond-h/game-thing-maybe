import test from 'ava';

import * as rxjs from 'rxjs';
import { TestScheduler } from 'rxjs/testing';

import { forkByKey } from '../src/util';

test('forkByKey acts as observable operator', t => {
  const inObs = rxjs.of();

  const outObs = inObs.pipe(forkByKey(() => rxjs.of()));

  t.is(typeof outObs.subscribe, 'function');
});

test('forkByKey works', t => {
  const scheduler = new TestScheduler((expected, actual) => {
    t.deepEqual(actual, expected);
  });

  scheduler.run(m => {
    const outerVals = {
      a: ['inner1'],
      b: ['inner1', 'inner2'],
      c: ['inner2'],
      d: []
    };
    const outer =  m.cold('-d--a--b---c--d-|', outerVals);
    const inner1Subs =    '----^------!-----';
    const inner2Subs =    '-------^------!--';
    const expected =      '----1-2a3b4c-d--|';
    const inner1 =     m.cold('1-2-3-4-5-6-7');
    const inner2 =        m.cold('a-b-c-d-e-f-g-h-l');

    const op = forkByKey(val => val === 'inner1' ? inner1 : inner2);

    m.expectObservable(outer.pipe(op)).toBe(expected);
    m.expectSubscriptions(inner1.subscriptions).toBe(inner1Subs);
    m.expectSubscriptions(inner2.subscriptions).toBe(inner2Subs);
  });

  t.pass();
});

test('forkByKey cleans up on completion of outer', t => {
  const scheduler = new TestScheduler((expected, actual) => {
    t.deepEqual(actual, expected);
  });

  scheduler.run(m => {
    const outerVals = {
      a: ['inner1'],
      b: ['inner1', 'inner2'],
      c: ['inner2'],
      d: []
    };
    const outer =  m.cold('-d--a--b---c|', outerVals);
    const inner1Subs =    '----^------!-';
    const inner2Subs =    '-------^----!';
    const expected =      '----1-2a3b4c|';
    const inner1 = m.cold('1-2-3-4-5-6-7-8-9');
    const inner2 = m.cold('a-b-c-d-e-f-g-h-l');

    const op = forkByKey(val => val === 'inner1' ? inner1 : inner2);

    m.expectObservable(outer.pipe(op)).toBe(expected);
    m.expectSubscriptions(inner1.subscriptions).toBe(inner1Subs);
    m.expectSubscriptions(inner2.subscriptions).toBe(inner2Subs);
  });

  t.pass();
});

test('forkByKey ignores completion of inner observables', t => {
  const scheduler = new TestScheduler((expected, actual) => {
    t.deepEqual(actual, expected);
  });

  scheduler.run(m => {
    const outerVals = {
      a: ['inner1'],
      b: ['inner1', 'inner2'],
      c: ['inner2'],
      d: []
    };
    const outer =  m.cold('-d--a--b---c--d-|', outerVals);
    const inner1Subs =    '----^---!--------';
    const inner2Subs =    '-------^------!--';
    const expected =      '----1-2a-b-c-d--|';
    const inner1 =     m.cold('1-2-|');
    const inner2 =        m.cold('a-b-c-d-e-f-g-h-l');

    const op = forkByKey(val => val === 'inner1' ? inner1 : inner2);

    m.expectObservable(outer.pipe(op)).toBe(expected);
    m.expectSubscriptions(inner1.subscriptions).toBe(inner1Subs);
    m.expectSubscriptions(inner2.subscriptions).toBe(inner2Subs);
  });

  t.pass();
});
test('forkByKey cleans up on error in outer', t => {
  const scheduler = new TestScheduler((expected, actual) => {
    t.deepEqual(actual, expected);
  });

  scheduler.run(m => {
    const outerVals = {
      a: ['inner1'],
      b: ['inner1', 'inner2'],
      c: ['inner2'],
      d: []
    };
    const outer =  m.cold('-d--a--b--#', outerVals);
    const inner1Subs =    '----^-----!';
    const inner2Subs =    '-------^--!';
    const expected =      '----1-2a3b#';
    const inner1 = m.cold('1-2-3-4-5-6-7-8-9');
    const inner2 = m.cold('a-b-c-d-e-f-g-h-l');

    const op = forkByKey(val => val === 'inner1' ? inner1 : inner2);

    m.expectObservable(outer.pipe(op)).toBe(expected);
    m.expectSubscriptions(inner1.subscriptions).toBe(inner1Subs);
    m.expectSubscriptions(inner2.subscriptions).toBe(inner2Subs);
  });

  t.pass();
});


test('forkByKey cleans up on error in inner', t => {
  const scheduler = new TestScheduler((expected, actual) => {
    t.deepEqual(actual, expected);
  });

  scheduler.run(m => {
    const outerVals = {
      a: ['inner1'],
      b: ['inner1', 'inner2'],
      c: ['inner2'],
      d: []
    };
    const outer =  m.cold('-d--a--b---c|', outerVals);
    const outerSubs =     '^-------!';
    const inner1Subs =    '----^---!';
    const inner2Subs =    '-------^!';
    const expected =      '----1-2a#';
    const inner1 = m.cold('1-2-#');
    const inner2 = m.cold('a-b-c-d-e-f-g-h-l');

    const op = forkByKey(val => val === 'inner1' ? inner1 : inner2);

    m.expectObservable(outer.pipe(op)).toBe(expected);
    m.expectSubscriptions(outer.subscriptions).toBe(outerSubs);
    m.expectSubscriptions(inner1.subscriptions).toBe(inner1Subs);
    m.expectSubscriptions(inner2.subscriptions).toBe(inner2Subs);
  });

  t.pass();
});


