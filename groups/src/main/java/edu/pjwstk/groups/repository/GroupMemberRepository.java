package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;

import java.util.Optional;
import java.util.UUID;

public interface GroupMemberRepository {
    Optional<GroupMember> findById(Integer groupMemberId);

    boolean existsByUserIdAndGroup(Group group, UUID userId);

    GroupMember save(GroupMember groupMember);
}
