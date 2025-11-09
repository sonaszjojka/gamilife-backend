package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupMemberJpaRepository extends JpaRepository<GroupMember, UUID> {
    boolean existsByUserIdAndGroup(UUID userId, Group memberGroup);

    Optional<GroupMember> findByUserIdAndGroup(UUID userId, Group memberGroup);

    Optional<GroupMember> findByGroupMemberIdAndGroupId(UUID groupMemberId, UUID groupId);

    @EntityGraph(attributePaths = {"group"})
    Optional<GroupMember> findWithGroupByGroupMemberIdAndGroupId(UUID groupMemberId, UUID groupId);
}
