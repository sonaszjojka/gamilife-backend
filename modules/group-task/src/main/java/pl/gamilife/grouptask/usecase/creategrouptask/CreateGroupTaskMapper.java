package pl.gamilife.grouptask.usecase.creategrouptask;

import pl.gamilife.grouptask.entity.GroupTask;

import java.util.UUID;

public interface CreateGroupTaskMapper {

    GroupTask toEntity(CreateGroupTaskRequest req,
                       UUID groupTaskId,
                       UUID groupId,
                       UUID taskId
    );

    CreateGroupTaskResponse toResponse(GroupTask groupTask);

}
