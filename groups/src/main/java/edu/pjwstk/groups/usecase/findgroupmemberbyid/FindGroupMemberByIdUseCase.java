package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.groups.shared.GroupMemberDto;

public interface FindGroupMemberByIdUseCase {
    GroupMemberDto execute(Integer groupMemberId);
}
