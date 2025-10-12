package edu.pjwstk.groups.domain;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;


@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_request_status")
public class GroupRequestStatus {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "group_request_status_id", nullable = false, updatable = false, unique = true)
    private Integer groupRequestStatusId;

    @Column(name = "title", length = 100, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "groupRequestStatus")
    @ToString.Exclude
    private List<GroupRequest> groupRequests;
}
