package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.InvitationStatus;
import org.springframework.data.jpa.repository.JpaRepository;

public interface InvitationStatusJpaRepository extends JpaRepository<InvitationStatus, Integer> {
}
