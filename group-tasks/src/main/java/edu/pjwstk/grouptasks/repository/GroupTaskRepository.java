package edu.pjwstk.grouptasks.repository;


import edu.pjwstk.grouptasks.entity.GroupTask;

import java.util.Optional;
import java.util.UUID;

public interface GroupTaskRepository   {

    GroupTask save(GroupTask groupTask);
    void deleteByGroupTaskId(UUID groupTaskId);
    boolean existsByGroupTaskId(UUID groupTaskId);
    Optional<GroupTask> findByGroupTaskId(UUID groupTaskId);
}
