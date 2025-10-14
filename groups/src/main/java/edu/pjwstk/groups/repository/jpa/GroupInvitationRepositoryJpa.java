package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.entity.GroupInvitation;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupInvitationRepositoryJpa extends JpaRepository<GroupInvitation, UUID> {
}
