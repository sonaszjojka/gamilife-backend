package edu.pjwstk.grouptasks.usecase.editgrouptask;

import edu.pjwstk.grouptasks.entity.GroupTask;
import org.springframework.stereotype.Component;

@Component
public class EditGroupTaskMapperImpl implements EditGroupTaskMapper {


    @Override
    public EditGroupTaskResponse toResponse(GroupTask groupTask) {
        return EditGroupTaskResponse.builder()
                .groupTaskId(groupTask.getGroupTaskId())
                .taskId(groupTask.getTaskId())
                .groupId(groupTask.getGroupId())
                .reward(groupTask.getReward())
                .isAccepted(groupTask.getIsAccepted())
                .acceptedDate(groupTask.getAcceptedDate())
                .declineMessage(groupTask.getDeclineMessage())
                .build();
    }
}
