package pl.gamilife.grouptask.usecase.editgrouptaskmember;

import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.entity.GroupTaskMember;

@Component
public class EditGroupTaskMemberMapperImpl implements EditGroupTaskMemberMapper {
    @Override
    public EditGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember) {
        return EditGroupTaskMemberResponse.builder()
                .groupTaskMemberId(groupTaskMember.getId())
                .groupMemberId(groupTaskMember.getGroupMemberId())
                .groupTaskId(groupTaskMember.getGroupTaskId())
                .isMarkedDone(groupTaskMember.isMarkedDone())
                .build();
    }
}
