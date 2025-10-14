package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.domain.GroupType;
import edu.pjwstk.groups.repository.GroupTypeRepository;
import edu.pjwstk.groups.repository.jpa.GroupTypeRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class GroupTypeRepositoryImpl implements GroupTypeRepository {
    private final GroupTypeRepositoryJpa repositoryJpa;

    public GroupTypeRepositoryImpl(GroupTypeRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<GroupType> findById(Integer groupTypeId) {
        return repositoryJpa.findById(groupTypeId);
    }
}
