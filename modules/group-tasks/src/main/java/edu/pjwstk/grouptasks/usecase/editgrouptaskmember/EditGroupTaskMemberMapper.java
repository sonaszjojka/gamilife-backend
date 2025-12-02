package edu.pjwstk.grouptasks.usecase.editgrouptaskmember;

import edu.pjwstk.grouptasks.entity.GroupTaskMember;

public interface EditGroupTaskMemberMapper {
    EditGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember);
}
