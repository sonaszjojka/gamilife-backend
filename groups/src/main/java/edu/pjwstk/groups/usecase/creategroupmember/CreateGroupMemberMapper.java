package edu.pjwstk.groups.usecase.creategroupmember;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;

import java.util.UUID;

public interface CreateGroupMemberMapper {
    GroupMember toEntity(UUID userId, Group group, UUID groupMemberId);

    CreateGroupMemberResponse toResponse(GroupMember savedGroupMember);
}
