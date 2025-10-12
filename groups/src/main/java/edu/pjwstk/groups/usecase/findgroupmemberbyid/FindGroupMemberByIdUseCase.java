package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;

public interface FindGroupMemberByIdUseCase {
    GroupMemberDto execute(Integer groupMemberId);
}
