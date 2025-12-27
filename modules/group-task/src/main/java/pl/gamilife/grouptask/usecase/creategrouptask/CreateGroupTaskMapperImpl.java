package pl.gamilife.grouptask.usecase.creategrouptask;

import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.entity.GroupTask;

import java.util.UUID;

@Component
public class CreateGroupTaskMapperImpl implements CreateGroupTaskMapper {
    @Override
    public GroupTask toEntity(CreateGroupTaskRequest req, UUID groupId, UUID taskId) {
        return GroupTask.create(
                taskId,
                groupId,
                req.reward()
        );
    }

    @Override
    public CreateGroupTaskResponse toResponse(GroupTask groupTask) {
        return CreateGroupTaskResponse.builder()
                .groupTaskId(groupTask.getId())
                .taskId(groupTask.getTaskId())
                .groupId(groupTask.getGroupId())
                .reward(groupTask.getReward())
                .isAccepted(groupTask.isAccepted())
                .acceptedDate(groupTask.getAcceptedAt())
                .declineMessage(groupTask.getDeclineMessage())
                .lastEdit(groupTask.getUpdatedAt())
                .build();
    }
}
