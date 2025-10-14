package edu.pjwstk.grouptasks.usecase.creategrouptask;
import java.util.UUID;
import edu.pjwstk.grouptasks.entity.GroupTask;

public interface CreateGroupTaskMapper {

    GroupTask toEntity(CreateGroupTaskRequest req,
                       UUID groupTaskId,
                       UUID taskId
    );

    CreateGroupTaskResponse toResponse(GroupTask groupTask);

}
