package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.domain.Group;

import java.util.Optional;
import java.util.UUID;

public interface GroupRepository {
    Optional<Group> findById(UUID groupId);
}
