package edu.pjwstk.grouptasks.usecase.creategrouptask;

import edu.pjwstk.grouptasks.entity.GroupTask;
import org.springframework.stereotype.Component;

import java.util.UUID;
@Component
public class CreateGroupTaskMapperImpl implements CreateGroupTaskMapper {
    @Override
    public GroupTask toEntity(CreateGroupTaskRequest req, UUID groupTaskId, UUID taskId) {
        return GroupTask.builder()
                .groupTaskId(groupTaskId)
                .taskId(taskId)
                .reward(req.reward())
                .isAccepted(req.isAccepted())
                .acceptedDate(req.acceptedDate())
                .declineMessage(req.declineMessage())
                .build();
    }

    @Override
    public CreateGroupTaskResponse toResponse(GroupTask groupTask) {
        return CreateGroupTaskResponse.builder()
                .groupTaskId(groupTask.getGroupTaskId())
                .taskId(groupTask.getTaskId())
                .reward(groupTask.getReward())
                .isAccepted(groupTask.getIsAccepted())
                .acceptedDate(groupTask.getAcceptedDate())
                .declineMessage(groupTask.getDeclineMessage())
                .lastEdit(groupTask.getLastEdit())
                .build();
    }
}
