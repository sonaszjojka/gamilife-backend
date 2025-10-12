package edu.pjwstk.groups.shared;

import edu.pjwstk.groups.domain.GroupMember;

public interface GroupMemberMapper {
    GroupMemberDto toResponse(GroupMember groupMember);
}
