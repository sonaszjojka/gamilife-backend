package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.entity.GroupTaskMember;

import java.util.UUID;

public interface CreateGroupTaskMemberMapper {

    GroupTaskMember toEntity ( GroupTask groupTask, UUID groupMemberId, UUID groupTaskMemberId);
    CreateGroupTaskMemberResponse toResponse (GroupTaskMember groupTaskMember);

}
