package pl.gamilife.grouptask.usecase.editgrouptaskmember;

import pl.gamilife.grouptask.entity.GroupTaskMember;
import org.springframework.stereotype.Component;

@Component
public class EditGroupTaskMemberMapperImpl implements EditGroupTaskMemberMapper {
    @Override
    public EditGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember) {
        return EditGroupTaskMemberResponse.builder()
                .groupTaskMemberId(groupTaskMember.getGroupTaskMemberId())
                .groupMemberId(groupTaskMember.getGroupMemberId())
                .groupTaskId(groupTaskMember.getGroupTaskId().getGroupTaskId())
                .isMarkedDone(groupTaskMember.getIsMarkedDone())
                .build();
    }
}
