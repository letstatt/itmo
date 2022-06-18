/**
 * Homework for Java Advanced course.
 *
 */
module info.kgeorgiy.ja.yakupov {
    requires java.compiler; // for implementor
    requires info.kgeorgiy.java.advanced.walk;
    requires info.kgeorgiy.java.advanced.arrayset;
    requires info.kgeorgiy.java.advanced.student;
    requires info.kgeorgiy.java.advanced.implementor;
    requires info.kgeorgiy.java.advanced.concurrent;
    requires info.kgeorgiy.java.advanced.mapper;
    requires info.kgeorgiy.java.advanced.crawler;
    requires info.kgeorgiy.java.advanced.hello;
    requires java.rmi;
    requires jdk.httpserver;

    exports info.kgeorgiy.ja.yakupov.bank.person;
    exports info.kgeorgiy.ja.yakupov.bank.account;
    exports info.kgeorgiy.ja.yakupov.bank.bank;
    exports info.kgeorgiy.ja.yakupov.bank.utils;
    exports info.kgeorgiy.ja.yakupov.bank;
}