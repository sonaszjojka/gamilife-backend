package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.entity.InvitationStatus;
import org.springframework.data.jpa.repository.JpaRepository;

public interface InvitationStatusRepositoryJpa extends JpaRepository<InvitationStatus, Integer> {
}
