package info.kgeorgiy.ja.yakupov.student;

import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.GroupQuery;
import info.kgeorgiy.java.advanced.student.Student;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StudentDB implements GroupQuery {

    // CONSTANTS

    private static final Comparator<Student> COMPARE_BY_NAME =
            Comparator.comparing(Student::getLastName)
                    .thenComparing(Student::getFirstName)
                    .reversed()
                    .thenComparing(Student::getId);

    private static final Predicate<Student> PASS_EVERYONE = s -> true;

    private static final Function<Student, Student> IDENTITY = s -> s;

    // MAPPING

    private <S, R, T> R mapToCollection(final Stream<S> stream, final Function<S, T> f, final Collector<T, ?, R> collector) {
        return stream.map(f).collect(collector);
    }

    private <S, T> List<T> mapToList(final Stream<S> stream, final Function<S, T> f) {
        return mapToCollection(stream, f, Collectors.toList());
    }

    private <S, T> List<T> mapToList(final List<S> list, final Function<S, T> f) {
        return mapToList(list.stream(), f);
    }

    // FILTERING

    private Stream<Student> filter(final Collection<Student> col, final Predicate<Student> pred) {
        return col.stream().filter(pred);
    }

    private List<Student> filterToListSortedBy(final Collection<Student> col, final Predicate<Student> pred, final Comparator<Student> cmp) {
        return filter(col, pred).sorted(cmp).collect(Collectors.toList());
    }

    private List<Student> filterToListSortedByName(final Collection<Student> col, final Predicate<Student> pred) {
        return filterToListSortedBy(col, pred, COMPARE_BY_NAME);
    }

    // UTILITIES

    private <T> List<Student> getStudentsBy(final Collection<Student> students, final Function<Student, T> f, final T param) {
        return filterToListSortedByName(students, s -> f.apply(s).equals(param));
    }

    private <D> Stream<Map.Entry<GroupName, D>> groupingByGroupName(final Collection<Student> students, final Collector<Student, ?, D> collector) {
        return mapToCollection(students.stream(), IDENTITY,
                Collectors.groupingBy(Student::getGroup, collector)).entrySet().stream();
    }

    private List<Group> sortedGroups(final Collection<Student> students, final UnaryOperator<List<Student>> cmp) {
        return mapToList(groupingByGroupName(students, Collectors.toList()).sorted(
                        Map.Entry.comparingByKey(GroupName::compareTo)),
                e -> new Group(e.getKey(), cmp.apply(e.getValue())));
    }

    private <T> GroupName getMaxGroup(final Collection<Student> s, final Collector<Student, ?, T> collector, final Comparator<T> f, final Comparator<GroupName> g) {
        return groupingByGroupName(s, collector).max(
                Map.Entry.<GroupName, T>comparingByValue(f).thenComparing(
                        Map.Entry.comparingByKey(g))).map(Map.Entry::getKey).orElse(null);
    }

    // GROUP QUERY INTERFACE

    @Override
    public List<Group> getGroupsByName(final Collection<Student> students) {
        return sortedGroups(students, this::sortStudentsByName);
    }

    @Override
    public List<Group> getGroupsById(final Collection<Student> students) {
        return sortedGroups(students, this::sortStudentsById);
    }

    @Override
    public GroupName getLargestGroup(final Collection<Student> students) {
        return getMaxGroup(students, Collectors.counting(), Long::compareTo, GroupName::compareTo);
    }

    @Override
    public GroupName getLargestGroupFirstName(final Collection<Student> students) {
        return getMaxGroup(
                students,
                Collectors.collectingAndThen(
                        Collectors.mapping(Student::getFirstName, Collectors.toCollection(HashSet::new)),
                        HashSet::size),
                Integer::compareTo,
                Collections.reverseOrder(GroupName::compareTo));
    }

    // STUDENT QUERY INTERFACE

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return mapToList(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return mapToList(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        return mapToList(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return mapToList(students, s -> s.getFirstName() + " " + s.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return mapToCollection(students.stream(), Student::getFirstName, Collectors.toSet());
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        return students.stream().max(Student::compareTo).map(Student::getFirstName).orElse("");
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return filterToListSortedBy(students, PASS_EVERYONE, Student::compareTo);
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return filterToListSortedByName(students, PASS_EVERYONE);
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, final String name) {
        return getStudentsBy(students, Student::getFirstName, name);
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, final String name) {
        return getStudentsBy(students, Student::getLastName, name);
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students, final GroupName group) {
        return getStudentsBy(students, Student::getGroup, group);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, final GroupName group) {
        return filter(students, s -> s.getGroup().equals(group)).collect(
                Collectors.toMap(Student::getLastName, Student::getFirstName, BinaryOperator.minBy(String::compareTo)));
    }
}
