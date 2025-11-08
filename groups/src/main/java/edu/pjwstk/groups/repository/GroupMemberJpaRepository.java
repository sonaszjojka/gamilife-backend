package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupMemberJpaRepository extends JpaRepository<GroupMember, UUID> {
    boolean existsByUserIdAndMemberGroup(UUID userId, Group memberGroup);

    Integer countByMemberGroup(Group memberGroup);

    Optional<GroupMember> findByUserIdAndMemberGroup(UUID userId, Group memberGroup);

    Optional<GroupMember> findByGroupMemberIdAndMemberGroup_GroupId(UUID groupMemberId, UUID groupId);
}
