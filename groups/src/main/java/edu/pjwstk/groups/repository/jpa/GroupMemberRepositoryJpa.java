package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupMemberRepositoryJpa extends JpaRepository<GroupMember, Integer> {
    boolean existsByUserIdAndMemberGroup(UUID userId, Group memberGroup);

    Integer countByMemberGroup(Group memberGroup);
}
