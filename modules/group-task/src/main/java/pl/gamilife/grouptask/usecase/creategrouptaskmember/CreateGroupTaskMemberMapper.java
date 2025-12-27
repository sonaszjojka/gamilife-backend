package pl.gamilife.grouptask.usecase.creategrouptaskmember;

import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;

import java.util.UUID;

public interface CreateGroupTaskMemberMapper {

    GroupTaskMember toEntity(GroupTask groupTask, UUID groupMemberId);

    CreateGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember);

}
