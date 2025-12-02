package pl.gamilife.grouptask.usecase.creategrouptask;
import java.util.UUID;
import pl.gamilife.grouptask.entity.GroupTask;

public interface CreateGroupTaskMapper {

    GroupTask toEntity(CreateGroupTaskRequest req,
                       UUID groupTaskId,
                       UUID groupId,
                       UUID taskId
    );

    CreateGroupTaskResponse toResponse(GroupTask groupTask);

}
