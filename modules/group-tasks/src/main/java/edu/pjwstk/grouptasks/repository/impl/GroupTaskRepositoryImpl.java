package edu.pjwstk.grouptasks.repository.impl;

import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import edu.pjwstk.grouptasks.repository.jpa.GroupTaskRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupTaskRepositoryImpl implements GroupTaskRepository {
    private final GroupTaskRepositoryJpa groupTaskRepositoryJpa;

    public GroupTaskRepositoryImpl(GroupTaskRepositoryJpa groupTaskRepositoryJpa) {
        this.groupTaskRepositoryJpa = groupTaskRepositoryJpa;
    }

    @Override
    public GroupTask save(GroupTask groupTask) {
        return groupTaskRepositoryJpa.save(groupTask);
    }

    @Override
    public void deleteByGroupTaskId(UUID groupTaskId) {
        groupTaskRepositoryJpa.deleteById(groupTaskId);
    }


    @Override
    public Optional<GroupTask> findByGroupTaskId(UUID groupTaskId) {
        return groupTaskRepositoryJpa.findById(groupTaskId);
    }
}
