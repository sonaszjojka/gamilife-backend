package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

public interface GroupMemberJpaRepository extends JpaRepository<GroupMember, UUID> {
    boolean existsByUserIdAndGroup(UUID userId, Group memberGroup);

    Optional<GroupMember> findByUserIdAndGroup(UUID userId, Group memberGroup);

    Optional<GroupMember> findByGroupMemberIdAndGroupId(UUID groupMemberId, UUID groupId);

    @EntityGraph(attributePaths = {"group"})
    Optional<GroupMember> findWithGroupByGroupMemberIdAndGroupId(UUID groupMemberId, UUID groupId);

    @Query("""
                SELECT gm
                FROM GroupMember gm
                WHERE gm.userId = :userId
                  AND gm.group = :group
                  AND gm.leftAt IS NULL
            """)
    Optional<GroupMember> findActiveMember(UUID userId, Group group);
}
