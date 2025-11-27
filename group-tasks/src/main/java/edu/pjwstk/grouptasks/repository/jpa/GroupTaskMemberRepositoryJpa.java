package edu.pjwstk.grouptasks.repository.jpa;

import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupTaskMemberRepositoryJpa extends JpaRepository<GroupTaskMember, UUID> {
}
