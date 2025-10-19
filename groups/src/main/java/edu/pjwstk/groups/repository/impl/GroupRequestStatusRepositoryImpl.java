package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.repository.GroupRequestStatusRepository;
import edu.pjwstk.groups.repository.jpa.GroupRequestStatusRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class GroupRequestStatusRepositoryImpl implements GroupRequestStatusRepository {

    private final GroupRequestStatusRepositoryJpa repositoryJpa;

    public GroupRequestStatusRepositoryImpl(GroupRequestStatusRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<GroupRequestStatus> findById(Integer id) {
        return repositoryJpa.findById(id);
    }
}
