package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.domain.GroupMember;
import org.springframework.data.jpa.repository.JpaRepository;

public interface GroupMemberRepositoryJpa extends JpaRepository<GroupMember, Integer> {
}
