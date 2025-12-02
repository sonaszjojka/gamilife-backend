package pl.gamilife.grouptask.usecase.editgrouptaskmember;

import pl.gamilife.grouptask.entity.GroupTaskMember;

public interface EditGroupTaskMemberMapper {
    EditGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember);
}
