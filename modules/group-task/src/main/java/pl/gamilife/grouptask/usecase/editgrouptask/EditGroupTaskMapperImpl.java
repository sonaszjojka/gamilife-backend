package pl.gamilife.grouptask.usecase.editgrouptask;

import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.entity.GroupTask;

@Component
public class EditGroupTaskMapperImpl implements EditGroupTaskMapper {


    @Override
    public EditGroupTaskResponse toResponse(GroupTask groupTask) {
        return EditGroupTaskResponse.builder()
                .groupTaskId(groupTask.getId())
                .taskId(groupTask.getTaskId())
                .groupId(groupTask.getGroupId())
                .reward(groupTask.getReward())
                .isAccepted(groupTask.isAccepted())
                .acceptedDate(groupTask.getAcceptedAt())
                .declineMessage(groupTask.getDeclineMessage())
                .build();
    }
}
