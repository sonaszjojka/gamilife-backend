package pl.gamilife.grouptask.repository;


import pl.gamilife.grouptask.entity.GroupTask;

import java.util.Optional;
import java.util.UUID;

public interface GroupTaskRepository {

    GroupTask save(GroupTask groupTask);

    void deleteByGroupTaskId(UUID groupTaskId);

    Optional<GroupTask> findByGroupTaskId(UUID groupTaskId);
}
