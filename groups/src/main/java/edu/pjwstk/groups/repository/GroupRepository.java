package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.Group;

import java.util.Optional;
import java.util.UUID;

public interface GroupRepository {
    Optional<Group> findById(UUID groupId);

    Group save(Group group);

    void deleteById(UUID groupId);
}
