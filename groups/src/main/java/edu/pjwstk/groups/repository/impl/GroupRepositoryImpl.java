package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.domain.Group;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.jpa.GroupRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupRepositoryImpl implements GroupRepository {

    private final GroupRepositoryJpa groupRepositoryJpa;

    public GroupRepositoryImpl(GroupRepositoryJpa groupRepositoryJpa) {
        this.groupRepositoryJpa = groupRepositoryJpa;
    }

    @Override
    public Optional<Group> findById(UUID groupId) {
        return groupRepositoryJpa.findById(groupId);
    }

    @Override
    public Group save(Group group) {
        return groupRepositoryJpa.save(group);
    }
}
