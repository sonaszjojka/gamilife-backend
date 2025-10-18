package edu.pjwstk.groups.usecase.creategroupmember;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;

import java.util.UUID;

public interface CreateGroupMemberMapper {
    GroupMember toEntity(UUID userId, Group group);

    CreateGroupMemberResponse toResponse(GroupMember savedGroupMember);
}
