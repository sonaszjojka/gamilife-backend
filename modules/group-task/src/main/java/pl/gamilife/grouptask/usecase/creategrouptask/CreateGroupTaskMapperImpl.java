package pl.gamilife.grouptask.usecase.creategrouptask;

import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.entity.GroupTask;

import java.util.UUID;

@Component
public class CreateGroupTaskMapperImpl implements CreateGroupTaskMapper {
    @Override
    public GroupTask toEntity(CreateGroupTaskRequest req, UUID groupTaskId, UUID groupId, UUID taskId) {
        return GroupTask.builder()
                .groupTaskId(groupTaskId)
                .taskId(taskId)
                .groupId(groupId)
                .reward(req.reward())
                .isAccepted(null)
                .acceptedDate(null)
                .declineMessage(null)
                .build();
    }

    @Override
    public CreateGroupTaskResponse toResponse(GroupTask groupTask) {
        return CreateGroupTaskResponse.builder()
                .groupTaskId(groupTask.getGroupTaskId())
                .taskId(groupTask.getTaskId())
                .groupId(groupTask.getGroupId())
                .reward(groupTask.getReward())
                .isAccepted(groupTask.getIsAccepted())
                .acceptedDate(groupTask.getAcceptedDate())
                .declineMessage(groupTask.getDeclineMessage())
                .lastEdit(groupTask.getLastEdit())
                .build();
    }
}
