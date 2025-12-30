package pl.gamilife.grouptask.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.repository.GroupTaskRepository;
import pl.gamilife.grouptask.repository.jpa.GroupTaskRepositoryJpa;

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

    @Override
    public Optional<GroupTask> findWithMembersByGroupTaskId(UUID groupTaskId) {
        return groupTaskRepositoryJpa.findWithMembersById(groupTaskId);
    }
}
