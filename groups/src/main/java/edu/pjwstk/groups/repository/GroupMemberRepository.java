package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.GroupMember;

import java.util.Optional;

public interface GroupMemberRepository {
    Optional<GroupMember> findById(Integer groupMemberId);
}
