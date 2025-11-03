package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;

import java.util.UUID;

public interface FindGroupMemberByIdUseCase {
    GroupMemberDto execute(UUID groupMemberId);
}
