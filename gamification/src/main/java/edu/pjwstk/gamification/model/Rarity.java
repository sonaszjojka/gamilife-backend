package edu.pjwstk.gamification.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.ToString;
import org.hibernate.annotations.Immutable;

import java.util.HashSet;
import java.util.Set;

@Getter
@Entity
@Table(name = "rarity")
@ToString(exclude = {"items"})
@Immutable
public class Rarity {
    @Id
    @Column(name = "id", nullable = false)
    private Integer id;

    @Size(max = 20)
    @NotNull
    @Column(name = "name", nullable = false, length = 20)
    private String name;

    @OneToMany(mappedBy = "rarity")
    private Set<Item> items = new HashSet<>();

}